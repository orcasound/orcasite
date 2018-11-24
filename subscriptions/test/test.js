const mocha = require('mocha');
const sut = require('../src/subscriptionService');
var sinon = require('sinon');
var should = require('should');
var r2 = require('r2');

var sub = {
    host: "foo.com"
};

var sub2 = {
    host: "bar.com"
} 

var notification = {
    "event": {
        "type": "dummy event"
    }
}

describe("subscriptionService", ()=> {
    describe('subscribe', ()=> {
        it('should add the subscription', ()=> {
            sut.subscribe(sub);
            sut.getSubscriptions()[0].should.be.equal(sub);
        });

        it('should ignore duplicate subscriptions', ()=> {
            sut.subscribe(sub);
            var result = sut.subscribe(sub);
            result.should.be.equal(false);
            sut.getSubscriptions().length.should.be.equal(1);
        });
    });

    describe('unsubscribe', ()=> {
        it('should remove the subscription', ()=> {
            sut.subscribe(sub);
            var result = sut.unsubscribe(sub);
            result.should.be.equal(true);
            sut.getSubscriptions().length.should.be.equal(0);
        });
    });

    describe('notify', ()=> {
        it('should invoke each subscriber', ()=> {
            var mock = sinon.mock(r2);
            var expect = mock.expects("post").
                twice().
                returns({response: {
                    json: ()=>"test"
                }});

            sut.r2(r2);
            sut.subscribe(sub);
            sut.subscribe(sub2);
            sut.notify(notification);
        });

    })
});
