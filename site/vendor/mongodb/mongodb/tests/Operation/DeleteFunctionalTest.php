<?php

namespace MongoDB\Tests\Operation;

use MongoDB\DeleteResult;
use MongoDB\Collection;
use MongoDB\Driver\BulkWrite;
use MongoDB\Driver\WriteConcern;
use MongoDB\Operation\Delete;
use MongoDB\Tests\CommandObserver;
use stdClass;

class DeleteFunctionalTest extends FunctionalTestCase
{
    private $collection;

    public function setUp()
    {
        parent::setUp();

        $this->collection = new Collection($this->manager, $this->getDatabaseName(), $this->getCollectionName());
    }

    public function testDeleteOne()
    {
        $this->createFixtures(3);

        $filter = ['_id' => 1];

        $operation = new Delete($this->getDatabaseName(), $this->getCollectionName(), $filter, 1);
        $result = $operation->execute($this->getPrimaryServer());

        $this->assertInstanceOf('MongoDB\DeleteResult', $result);
        $this->assertSame(1, $result->getDeletedCount());

        $expected = [
            ['_id' => 2, 'x' => 22],
            ['_id' => 3, 'x' => 33],
        ];

        $this->assertSameDocuments($expected, $this->collection->find());
    }

    public function testDeleteMany()
    {
        $this->createFixtures(3);

        $filter = ['_id' => ['$gt' => 1]];

        $operation = new Delete($this->getDatabaseName(), $this->getCollectionName(), $filter, 0);
        $result = $operation->execute($this->getPrimaryServer());

        $this->assertInstanceOf('MongoDB\DeleteResult', $result);
        $this->assertSame(2, $result->getDeletedCount());

        $expected = [
            ['_id' => 1, 'x' => 11],
        ];

        $this->assertSameDocuments($expected, $this->collection->find());
    }

    public function testSessionOption()
    {
        if (version_compare($this->getServerVersion(), '3.6.0', '<')) {
            $this->markTestSkipped('Sessions are not supported');
        }

        (new CommandObserver)->observe(
            function() {
                $operation = new Delete(
                    $this->getDatabaseName(),
                    $this->getCollectionName(),
                    [],
                    0,
                    ['session' => $this->createSession()]
                );

                $operation->execute($this->getPrimaryServer());
            },
            function(stdClass $command) {
                $this->assertObjectHasAttribute('lsid', $command);
            }
        );
    }

    public function testUnacknowledgedWriteConcern()
    {
        $filter = ['_id' => 1];
        $options = ['writeConcern' => new WriteConcern(0)];

        $operation = new Delete($this->getDatabaseName(), $this->getCollectionName(), $filter, 0, $options);
        $result = $operation->execute($this->getPrimaryServer());

        $this->assertFalse($result->isAcknowledged());

        return $result;
    }

    /**
     * @depends testUnacknowledgedWriteConcern
     * @expectedException MongoDB\Exception\BadMethodCallException
     * @expectedExceptionMessageRegExp /[\w:\\]+ should not be called for an unacknowledged write result/
     */
    public function testUnacknowledgedWriteConcernAccessesDeletedCount(DeleteResult $result)
    {
        $result->getDeletedCount();
    }

    /**
     * Create data fixtures.
     *
     * @param integer $n
     */
    private function createFixtures($n)
    {
        $bulkWrite = new BulkWrite(['ordered' => true]);

        for ($i = 1; $i <= $n; $i++) {
            $bulkWrite->insert([
                '_id' => $i,
                'x' => (integer) ($i . $i),
            ]);
        }

        $result = $this->manager->executeBulkWrite($this->getNamespace(), $bulkWrite);

        $this->assertEquals($n, $result->getInsertedCount());
    }
}
